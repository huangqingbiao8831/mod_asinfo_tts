/* 
 * FreeSWITCH Modular Media Switching Software Library / Soft-Switch Application
 * Copyright (C) 2005-2014, Anthony Minessale II <anthm@freeswitch.org>
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is FreeSWITCH Modular Media Switching Software Library / Soft-Switch Application
 *
 * The Initial Developer of the Original Code is
 * Anthony Minessale II <anthm@freeswitch.org>
 * Portions created by the Initial Developer are Copyright (C)
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * 
 * huangqb <hungqb@freeswitch.org>
 * 
 *
 * mod_asinfo_tts.c -- Flite Interface
 *
 */

        
/* Register Information Table */
#include <switch.h>
#include <switch_curl.h>
//#include <libswresample/swresample.h>
//#include <libavutil/avutil.h>
//#include <libavformat/avformat.h>



//#include <speex/speex_resampler.h>

SWITCH_MODULE_LOAD_FUNCTION(mod_asinfo_tts_load);
SWITCH_MODULE_SHUTDOWN_FUNCTION(mod_asinfo_tts_shutdown);
SWITCH_MODULE_DEFINITION(mod_asinfo_tts, mod_asinfo_tts_load, mod_asinfo_tts_shutdown, NULL);

#define HTTP_RECV_SEND_BUF_SIZE            (sizeof(char)*1024*2)    //2K
#define FLITE_BLOCK_SIZE                   (8192)               //
#define HTTP_STRING						 "http://"
#define HTTP_POST_ONE_SHOT_PIC_MEM			0
#define HTTP_BODY 							0  //http閸愬懎顔?
#define HTTP_HEAD							1  //http婢?
//#define   __FFMPEG__

typedef struct _stHttpUserInfo
{
	int nOperation;				//操作长度
	int ucFlag;					//用以区分头部还是内容部分
	int nSedSize;				//发送数据大小	
	int nRcvBufLen;				//缓冲区长度
	int nRcvSize;				//接收body数据大小
	
	char *pRcvBuffer;			//指向用户用来存储数据的buf
	char *pSendBuffer;			//指向用户发送数据的buf
}stHttpUserInfo;

typedef struct _stHttpClientInfo
{
	int nClientPort;			//http服务器监听端口号
	char chClientIP[32];		//http服务器IP
	char chClientPath[64];		//http服务器文件路径
	char chUserName[32];		//http服务器登录用户
	char chUserPassWord[32];	//http服务器登录密码
	char chUrl[128]; 			//http服务器url

	stHttpUserInfo stHttpUserHead;
	stHttpUserInfo stHttpUserBody;
}stHttpClientInfo;

static struct {
   char url[128];  //asinfo tts server url
   int native_rate; //tts输出的采样率
   double voice_speed;  //tts声音的播放速度
   double voice_volume; //tts声音的声量大小
} globals;


struct asinfo_tts_data {
    stHttpClientInfo *pHttpClientBuff;
	switch_buffer_t *audio_buffer;
	switch_queue_t  *tts_queue; /*conminication...*/
	switch_memory_pool_t *tts_pool;
	switch_thread_t *http_thread;
	switch_threadattr_t *thd_attr;
	int http_exit_flag; //http 线程退出flag,flag=1,表示线程退出了。
	//switch_file_t *pfile;
    
};

typedef struct asinfo_tts_data asinfo_tts_t;
#if 0
static void speex_resampler_buffs(switch_buffer_t **audio_buffer,void *ptr,size_t written);


static void speex_resampler_buffs(switch_buffer_t **audio_buffer,void *ptr,size_t written){

    SpeexResamplerState *resampler = NULL;
	size_t buffer_size = written;
    
    int err = 0;
    unsigned char* out_buffer = (unsigned char *)malloc(buffer_size);
	int ret = -1;

	//size_t inlen = written*0.5;
	size_t inlen = written;
    spx_uint32_t outlen = buffer_size;
    resampler = speex_resampler_init(1,24000,8000,10,&err);
    //speex_resampler_skip_zeros(resampler);



    //int samples = len;
    //int channel = 1;
   

   
    memset(out_buffer, 0,buffer_size);


    ret = speex_resampler_process_int(resampler,(spx_uint32_t)0,(const spx_int16_t*)ptr,(spx_uint32_t *)&inlen,(spx_int16_t *)out_buffer, &outlen);
    if (resampler!=NULL) {
        speex_resampler_destroy(resampler);
    }
    if(ret == RESAMPLER_ERR_SUCCESS)
    {
        //mpf_buffer_audio_write(synth_channel->audio_buffer, out_buffer,outlen*2);
		switch_buffer_create_dynamic(audio_buffer, FLITE_BLOCK_SIZE, outlen*2, 0);
		switch_assert(*audio_buffer);
        switch_buffer_write(*audio_buffer, out_buffer, outlen*2);

        free(out_buffer);
        out_buffer = NULL;
    }

}

#endif
#if 0
static void ffmpeg_resampler_buffs(switch_buffer_t **audio_buffer,void *ptr,int len){
    
    int inSampleRate=24000;
	char ptmp[8192]= {0};
	size_t tmpLen = 0;
    enum AVSampleFormat inSampleFmt = AV_SAMPLE_FMT_S16P;
    int inChLayout=1;
    
    int outSampleRate=8000;
    enum AVSampleFormat outSampleFmt = AV_SAMPLE_FMT_S16P;
    int outChLayout=1;
	int readlen = 0;

    
    // 输入缓冲区
    // 指向缓冲区的指针
    uint8_t **inData = NULL;
    // 缓冲区的大小
    int inLinesize = 0;
    // 声道数
    int inChs = av_get_channel_layout_nb_channels(inChLayout);
    // 一个样本的大小
    int inBytesPerSample = inChs * av_get_bytes_per_sample(inSampleFmt);
    // 缓冲区的样本数量
    int inSamples = len/inBytesPerSample;
    // 读取文件数据的大小
    

    // 输出缓冲区
    // 指向缓冲区的指针
    uint8_t **outData = NULL;
    // 缓冲区的大小
    int outLinesize = 0;
    // 声道数
    int outChs = av_get_channel_layout_nb_channels(outChLayout);
    // 一个样本的大小
    int outBytesPerSample = outChs * av_get_bytes_per_sample(outSampleFmt);
    // 缓冲区的样本数量
    int outSamples = av_rescale_rnd(outSampleRate, inSamples, inSampleRate, AV_ROUND_UP);

    // 返回结果
    int ret = 0;
    // 创建重采样上下文
    SwrContext *ctx = swr_alloc_set_opts(NULL,
                                         // 输出参数
                                         outChLayout, outSampleFmt, outSampleRate,
                                         // 输入参数
                                         inChLayout, inSampleFmt, inSampleRate,
                                         0, NULL);
    if (!ctx) {
        return;
    }

    // 初始化重采样上下文
    ret = swr_init(ctx);
    if (ret < 0) {
        return;
    }
    
    // 创建输入缓冲区
    ret = av_samples_alloc_array_and_samples(
              &inData,
              &inLinesize,
              inChs,
              inSamples,
              inSampleFmt,
              1);
    if (ret < 0) {
        return ;
    }

    // 创建输出缓冲区
    ret = av_samples_alloc_array_and_samples(
              &outData,
              &outLinesize,
              outChs,
              outSamples,
              outSampleFmt,
              1);
    
    //readlen = 0;
    //直接转换 流
    readlen = len;
    if (readlen > 0)
    {
        
       //int inlen = readlen;
        inSamples = readlen / inBytesPerSample;
        ret = swr_convert(ctx,
                          outData, outSamples,
                          (const uint8_t **)&ptr, inSamples
                         );
        
    }
    
    //取流
    //mpf_buffer_audio_write(synth_channel->audio_buffer, outData[0],ret * outBytesPerSample);
	//switch_buffer_create_dynamic(*audio_buffer, FLITE_BLOCK_SIZE, ret * outBytesPerSample, 0);
	//switch_assert(*audio_buffer);
    //switch_buffer_write(*audio_buffer,  outData[0], ret * outBytesPerSample);
	memcpy(ptmp,outData[0],ret*outBytesPerSample);
	tmpLen = ret*outBytesPerSample;
    while ((ret = swr_convert(ctx,outData, outSamples,NULL, 0)) > 0) {
        //取流
       // mpf_buffer_audio_write(synth_channel->audio_buffer, outData[0],ret * outBytesPerSample);
		//switch_buffer_write(*audio_buffer,  outData[0], ret * outBytesPerSample);
		memcpy(ptmp + tmpLen,outData[0],ret * outBytesPerSample);
		tmpLen += ret * outBytesPerSample;
    
    }
    switch_buffer_create_dynamic(audio_buffer, FLITE_BLOCK_SIZE, tmpLen, 0);
	switch_assert(*audio_buffer);
    switch_buffer_write(*audio_buffer, ptmp, tmpLen);


    
    // 释放输入缓冲区
    if (inData) {
        av_freep(&inData[0]);
    }
    av_freep(&inData);

    // 释放输出缓冲区
    if (outData) {
        av_freep(&outData[0]);
    }
    av_freep(&outData);
    swr_free(&ctx);

}
#endif
static size_t httpClient_writeBack(void *ptr, size_t size, size_t nmemb, void *stream)
{
	int writeLength = 0;
	int written  = size*nmemb;
	//switch_size_t len = written;
	//switch_speech_handle_t *sh = (switch_speech_handle_t *)stream;
	asinfo_tts_t *ptts = (asinfo_tts_t*)stream;
	stHttpUserInfo *pSt = &ptts->pHttpClientBuff->stHttpUserBody;	
	
	if(1 == pSt->ucFlag) //此处记录头部
	{
		if(pSt->pRcvBuffer != NULL && (written <= (pSt->nRcvBufLen - pSt->nRcvSize)))
		{
			writeLength = (written <= (pSt->nRcvBufLen - pSt->nRcvSize))?(written):(pSt->nRcvBufLen - pSt->nRcvSize);
			memcpy((char*)(pSt->pRcvBuffer) +pSt->nRcvSize, ptr, writeLength);
			pSt->nRcvSize += writeLength;
			switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "httpClient_writeBack write header:%d\n",writeLength);
		}
		return writeLength;
	}
	else if(0 == pSt->ucFlag)//此处记录body内容
	{
		//if(pSt->pRcvBuffer != NULL && (written <= (pSt->nRcvBufLen - pSt->nRcvSize)))
		//{
			//writeLength = (written <= (pSt->nRcvBufLen - pSt->nRcvSize))?(written):(pSt->nRcvBufLen - pSt->nRcvSize);
			//memcpy((char*)(pSt->pRcvBuffer) +pSt->nRcvSize, ptr, writeLength);
			//if ( ptts->onReadTTSstreamAudio ) ptts->onReadTTSstreamAudio(ptr,writeLength,(void*)stream);
			//pSt->nRcvSize += writeLength;
			//switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "------》httpClient_writeBack write data:%d\n",writeLength);
		//}
		if ( written > 0)
		{
			switch_buffer_t *audio_buffer = NULL;
      #ifndef __FFMPEG__
			switch_buffer_create_dynamic(&audio_buffer, FLITE_BLOCK_SIZE, written, 0);
			switch_assert(audio_buffer);
            switch_buffer_zero(audio_buffer);
            switch_buffer_write(audio_buffer, ptr, written);
	  #else
			//switch_file_write(ptts->pfile, (const void*)ptr, &len);
			//speex_resampler_buffs(&audio_buffer,ptr,written);
			ffmpeg_resampler_buffs(&audio_buffer,ptr,written);
	  #endif
            //switch_queue_push(ptts->tts_queue,(void*)audio_buffer);
	        if (switch_queue_trypush(ptts->tts_queue,(void*)audio_buffer) !=SWITCH_STATUS_SUCCESS)
	        {
                switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "------》httpClient_writeBack push msq error,so free the audio_buffer\n");
                switch_buffer_destroy(&audio_buffer);
				return 0;
	        }
			//switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "http write buffer :0x%ld.\n",(long)audio_buffer);
			return written;
		}

	}
	return 0;
}

#if 0
/*from http buffer read stream send to audio buffer*/
static int asinfo_tts_read_audio_call_back(void *pdata,size_t size,void *stream)
{
    size_t bytes_read;
	asinfo_tts_t *flite = (asinfo_tts_t *)stream;
	 
	
    switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_read_audio_call_back:enter function in top，size:%ld\n",size);
	if (!flite->audio_buffer) {
        switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_read_audio_call_back:enter flite->audio_buffer is null\n");
        if (size > 0) {
			switch_buffer_create_dynamic(&flite->audio_buffer, FLITE_BLOCK_SIZE, size, 0);
			switch_assert(flite->audio_buffer);
		}
	}

    
	if (pdata && (size > 0)) {
		switch_buffer_write(flite->audio_buffer, pdata, size);
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_read_audio_call_back:switch_buffer_write :%ld !!!\n",size);
	}

	if ((bytes_read = switch_buffer_read(flite->audio_buffer, data, *flite->sendSize))) {
		*(flite->sendSize) = bytes_read;
		flite->psendBuf += bytes_read;
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_read_audio_call_back: read bytes :%ld !!! return SWITCH_STATUS_SUCCESS\n",bytes_read);
		if (flite->audio_buffer) {
		   switch_buffer_destroy(&flite->audio_buffer);
	    }
		return SWITCH_STATUS_SUCCESS;
	}
    switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_read_audio_call_back:error !!!\n");
	return SWITCH_STATUS_FALSE;
	
}

#endif /** delete for comunication with queue*/
//static int httpClient_httpPost(stHttpClientInfo *pstUserArg,long *pRespCode)
static void *SWITCH_THREAD_FUNC httpClient_httpPost_run(switch_thread_t *thread,void *pdata)
{
	//int nRet = 0;
	switch_CURL *curl = NULL;
	switch_CURLcode code = (switch_CURLcode)0;
	long response_code = -1;
	struct curl_slist *chunk = NULL;
    asinfo_tts_t * ttsUserArg = (asinfo_tts_t*)pdata;
	stHttpClientInfo *pstUserArg = ttsUserArg->pHttpClientBuff;

	//*pRespCode = 0;
	if(strlen(pstUserArg->chUrl) < 14){
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "url error:%s\n",pstUserArg->chUrl);
		//nRet = -1;
		//return -1;
		goto exit_pos;
	}

	curl = switch_curl_easy_init();
	if(curl)
	{
		switch_curl_easy_setopt(curl, CURLOPT_URL, pstUserArg->chUrl);
		switch(pstUserArg->stHttpUserHead.nOperation)
		{
				case HTTP_POST_ONE_SHOT_PIC_MEM:
				//chunk = curl_slist_append(chunk, "Content-Type: image/jpeg;");
				//chunk = curl_slist_append(chunk, "Accept:application/json");
				chunk = curl_slist_append(chunk, "Content-Type: application/json");
				break;
			break;
			default:
				break;
		}
		switch_curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);
		switch_curl_easy_setopt(curl, CURLOPT_HEADER, 0L);		//分开接收head和body时，需要设置这个为0L
		switch_curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1L);	//注意，毫秒超时一定要设置这个  
		switch_curl_easy_setopt(curl, CURLOPT_POST, 1L); 

		switch_curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, httpClient_writeBack);
		curl_easy_setopt(curl, CURLOPT_WRITEDATA, &pstUserArg->stHttpUserBody); 	/*接收body部分*/
        switch_curl_easy_setopt(curl, CURLOPT_WRITEDATA, ttsUserArg);

		switch_curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION,httpClient_writeBack);
		curl_easy_setopt(curl, CURLOPT_WRITEHEADER, &pstUserArg->stHttpUserHead);	/*接收头部分*/
		switch_curl_easy_setopt(curl, CURLOPT_WRITEHEADER, ttsUserArg);
		
		//curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, 40*1000);	//设置延时90s
		//curl_easy_setopt( curl, CURLOPT_TIMEOUT, 90 ); 
		//curl_easy_setopt(curl, CURLOPT_LOW_SPEED_TIME, 3L); //songln suggest modify 
		//curl_easy_setopt(curl, CURLOPT_LOW_SPEED_LIMIT, 10L);// songln suggest modify
		switch_curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 90);
        switch_curl_easy_setopt(curl, CURLOPT_LOW_SPEED_LIMIT, 1);
        switch_curl_easy_setopt(curl, CURLOPT_LOW_SPEED_TIME, 90);
		switch_curl_easy_setopt(curl, CURLOPT_TIMEOUT, 120);
		
		switch(pstUserArg->stHttpUserHead.nOperation)
		{
			case HTTP_POST_ONE_SHOT_PIC_MEM:  // send text part
				switch_curl_easy_setopt(curl, CURLOPT_POSTFIELDS, pstUserArg->stHttpUserBody.pSendBuffer);
				switch_curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, pstUserArg->stHttpUserBody.nSedSize);
				break;
			default:
				break;
		}
		code = switch_curl_easy_perform(curl);
		if(code != CURLE_OK)
		{
			//nRet = -1;
			switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "func curl_easy_perform failed:error(%d)\n",code);
		} 
		else
		{
			switch_curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
			if(200 != response_code)
			{
				//nRet = -1;
				switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "func curl_easy_perform failed:response code (%ld)\n",response_code);
			}
		}
		switch_curl_slist_free_all(chunk);
		switch_curl_easy_cleanup(curl);

		//*pRespCode = response_code;
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "http thread will exit\n");
		ttsUserArg->http_exit_flag = 0;
        switch_thread_exit(thread, SWITCH_STATUS_SUCCESS);


	}
exit_pos:
	switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "func curl_easy_perform failed:unknow error\n");
	//return (void*)0;
	ttsUserArg->http_exit_flag = 0;
    switch_thread_exit(thread, SWITCH_STATUS_SUCCESS);
	return (void*)0;

}


static switch_status_t asinfo_tts_init_client_buffer(switch_speech_handle_t *sh,asinfo_tts_t *pas_tts)
{
	if ( NULL == pas_tts )
	{
        switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "user data pointer is NULL ,return failed\n");
		return SWITCH_STATUS_FALSE;
	}
	/*alloc user data...*/
	pas_tts->pHttpClientBuff = switch_core_alloc(sh->memory_pool,sizeof(stHttpClientInfo));
    if (NULL == pas_tts->pHttpClientBuff)
    {
       switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "http client data alloc error\n");
	   return SWITCH_STATUS_FALSE;
    }
	/*alloc http client recv and send buffer,szie = HTTP_RECV_SEND_BUF_SIZE...*/
	pas_tts->pHttpClientBuff->stHttpUserHead.pRcvBuffer = switch_core_alloc(sh->memory_pool,HTTP_RECV_SEND_BUF_SIZE);
	pas_tts->pHttpClientBuff->stHttpUserHead.pSendBuffer = switch_core_alloc(sh->memory_pool,HTTP_RECV_SEND_BUF_SIZE);
    pas_tts->pHttpClientBuff->stHttpUserBody.pRcvBuffer  = switch_core_alloc(sh->memory_pool,HTTP_RECV_SEND_BUF_SIZE);
	pas_tts->pHttpClientBuff->stHttpUserBody.pSendBuffer = switch_core_alloc(sh->memory_pool,HTTP_RECV_SEND_BUF_SIZE);
	if ( (NULL == pas_tts->pHttpClientBuff->stHttpUserHead.pRcvBuffer) ||
		   (NULL == pas_tts->pHttpClientBuff->stHttpUserHead.pSendBuffer) ||
		    (NULL == pas_tts->pHttpClientBuff->stHttpUserBody.pRcvBuffer) ||
		    (NULL == pas_tts->pHttpClientBuff->stHttpUserBody.pSendBuffer) )
	{
        switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "http client recv or send buffer alloc error\n");
		return SWITCH_STATUS_FALSE;
	}
    /*install callback function*/
    //pas_tts->onReadTTSstreamAudio = asinfo_tts_read_audio_call_back;
	/*copy url string to client data */
    strncpy(pas_tts->pHttpClientBuff->chUrl,globals.url,128);
	pas_tts->pHttpClientBuff->stHttpUserHead.nOperation 	= HTTP_POST_ONE_SHOT_PIC_MEM;		
	pas_tts->pHttpClientBuff->stHttpUserHead.ucFlag 		= HTTP_HEAD;	
	pas_tts->pHttpClientBuff->stHttpUserHead.nRcvBufLen 	= HTTP_RECV_SEND_BUF_SIZE;		
	pas_tts->pHttpClientBuff->stHttpUserHead.nRcvSize	= 0;		
	pas_tts->pHttpClientBuff->stHttpUserBody.nOperation	= HTTP_POST_ONE_SHOT_PIC_MEM;	
	pas_tts->pHttpClientBuff->stHttpUserBody.ucFlag 		= HTTP_BODY;	
	pas_tts->pHttpClientBuff->stHttpUserBody.nRcvBufLen	= HTTP_RECV_SEND_BUF_SIZE;		
	pas_tts->pHttpClientBuff->stHttpUserBody.nRcvSize	= 0;	
	pas_tts->pHttpClientBuff->stHttpUserBody.nSedSize	= 0;	
	return SWITCH_STATUS_SUCCESS;

}

static switch_status_t asinfo_tts_speech_open(switch_speech_handle_t *sh, const char *voice_name, int rate, int channels, switch_speech_flag_t *flags)
{
	asinfo_tts_t *flite = switch_core_alloc(sh->memory_pool, sizeof(asinfo_tts_t));
	//unsigned int fflags = 0;
	//switch_status_t stat;

	//sh->native_rate = 8000;
	//sh->native_rate = 24000;
	sh->native_rate = globals.native_rate;
	sh->samplerate = 8000;

	if (!voice_name) {
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "A voice is required. Valid voice names are awb, rms, slt or kal.\n");
		return SWITCH_STATUS_FALSE;
	}
    switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "voice name :%s.\n",voice_name);
	/*alloc pHttpClientBuff*/
    if (SWITCH_STATUS_FALSE == asinfo_tts_init_client_buffer(sh,flite))
	{
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_init_client_buffer error.\n");
		return SWITCH_STATUS_FALSE;
	}
	/*memory pool*/
	flite->tts_pool = sh->memory_pool;
	/*create queue...*/
	switch_queue_create(&flite->tts_queue, SWITCH_CORE_QUEUE_LEN, flite->tts_pool);
	/**init http_thread pointer...*/
	flite->http_thread = NULL;
	/*create pcm file*/


	//fflags |= SWITCH_FOPEN_CREATE;
	//fflags |= SWITCH_FOPEN_READ;
	//fflags |= SWITCH_FOPEN_WRITE;
	//fflags |= SWITCH_FOPEN_APPEND;

	//stat = switch_file_open(&flite->pfile,"./hello.pcm", fflags, SWITCH_FPROT_OS_DEFAULT, flite->tts_pool);
	//if (stat != SWITCH_STATUS_SUCCESS) {
	//	return SWITCH_STATUS_FALSE;
	//}
  
	if (flite->pHttpClientBuff) {
		sh->private_info = flite;
		return SWITCH_STATUS_SUCCESS;
	}

	return SWITCH_STATUS_FALSE;
}

static switch_status_t destroy_http_client_data(asinfo_tts_t *flite)
{
	return SWITCH_STATUS_SUCCESS;
}

static switch_status_t asinfo_tts_speech_close(switch_speech_handle_t *sh, switch_speech_flag_t *flags)
{
	asinfo_tts_t *flite = (asinfo_tts_t *) sh->private_info;
	void *pop = NULL;

	//if (flite->audio_buffer) {
	//	switch_buffer_destroy(&flite->audio_buffer);
	//}
    //switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_speech_close invoke\n");
	destroy_http_client_data(flite);
	/*if queue is not empty ,clear it*/
	while(switch_queue_pop_timeout(flite->tts_queue, &pop,20000) == SWITCH_STATUS_SUCCESS)
	{
		switch_buffer_t *audio_buffer = (switch_buffer_t *)pop;
        switch_buffer_destroy(&audio_buffer);
		pop = NULL;
	}
	/**delete queue...*/
    switch_queue_term(flite->tts_queue);
	/*close file...*/
	//switch_file_close(flite->pfile);
	*flags = SWITCH_SPEECH_FLAG_DONE|SWITCH_SPEECH_FLAG_FREE_POOL;
	//switch_thread_exit(flite->http_thread, SWITCH_STATUS_SUCCESS);
	//flite->http_thread = NULL; //should to be init
	while(flite->http_exit_flag) {//bug...等价http的请求结束，才能清理对应的内存，否则出现内存错误。
		//switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_speech_close invoke sleep...\n");
		sleep(1); //wait curl http client finish...
	}
	return SWITCH_STATUS_SUCCESS;
}

static switch_status_t asinfo_tts_speech_feed_tts(switch_speech_handle_t *sh, char *text, switch_speech_flag_t *flags)
{
	asinfo_tts_t *flite = (asinfo_tts_t *) sh->private_info;
	char *tts_text = NULL;

	if (flite->pHttpClientBuff)
	{
		if (flite->pHttpClientBuff->stHttpUserBody.pSendBuffer)
		{
			//tts_text = switch_mprintf("{\"text\":\"%s\",\"spk_id\":0,\"speed\":1.0,\"volume\":1.0,\"sample_rate\":16000,\"save_path\":\"./output.wav\"}",text);
			tts_text = switch_mprintf("{\"text\":\"%s\",\"spk_id\":0,\"speed\":%f,\"volume\":%f,\"sample_rate\":%d,\"save_path\":\"./output.wav\"}",text,globals.voice_speed,globals.voice_volume,globals.native_rate);
			strncpy(flite->pHttpClientBuff->stHttpUserBody.pSendBuffer,tts_text,HTTP_RECV_SEND_BUF_SIZE);
			flite->pHttpClientBuff->stHttpUserBody.nSedSize = (strlen(tts_text) < HTTP_RECV_SEND_BUF_SIZE)?strlen(tts_text):HTTP_RECV_SEND_BUF_SIZE;
			switch_safe_free(tts_text); //bug. tts_text must to be free...
		    return SWITCH_STATUS_SUCCESS;
		}
	}
   	return SWITCH_STATUS_FALSE;

}

static void asinfo_tts_speech_flush_tts(switch_speech_handle_t *sh)
{
	//asinfo_tts_t *flite = (asinfo_tts_t *) sh->private_info;
	switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_speech_flush_tts invoke\n");

	//if (flite->audio_buffer) {
	//	switch_buffer_zero(flite->audio_buffer);
	//}
	return;
}
/*
send post request
*/
static switch_status_t asinfo_tts_speech_read_tts(switch_speech_handle_t *sh, void *data, size_t *datalen, switch_speech_flag_t *flags)
{
	switch_size_t bytes_read;
	switch_size_t tmp_size = 8192;
	switch_interval_time_t timeout = 200000;
	//int nRet = -1;
	//long lTmp = -1;
	void *pop = NULL;
	asinfo_tts_t *flite = (asinfo_tts_t *) sh->private_info;
    /*create http thread*/
	if (!flite->http_thread)
	{
       switch_threadattr_create(&flite->thd_attr, flite->tts_pool);
	   flite->http_exit_flag = 1;//
	   switch_threadattr_detach_set(flite->thd_attr, 1);
	   switch_threadattr_stacksize_set(flite->thd_attr, SWITCH_THREAD_STACKSIZE);
	   switch_thread_create(&flite->http_thread, flite->thd_attr, httpClient_httpPost_run, (void*)flite, flite->tts_pool);
	}
	//switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_speech_read_tts :native_rate:%d,samplerate:%d\n",sh->native_rate,sh->samplerate);
	//switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_speech_read_tts :rate:%d,speed:%d,samples:%d,channels:%d\n",sh->rate,sh->speed,sh->samples,sh->channels);
	/*read msg from queue save to callback func buffer*/
    if (switch_queue_pop_timeout(flite->tts_queue, &pop,timeout) == SWITCH_STATUS_SUCCESS)
	{
		switch_buffer_t *audio_buffer = (switch_buffer_t *)pop;
		tmp_size = switch_buffer_len(audio_buffer);
		if ((bytes_read = switch_buffer_read(audio_buffer, data, tmp_size))) {
			*datalen = bytes_read;
			//switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "tts read buffer :0x%ld,readLen:%ld\n",(long)pop,*datalen);
			switch_buffer_destroy(&audio_buffer);
			return SWITCH_STATUS_SUCCESS;
	   }
	}
	switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_speech_read_tts return SWITCH_STATUS_FALSE\n");

	return SWITCH_STATUS_FALSE;
}

static void asinfo_tts_text_param_tts(switch_speech_handle_t *sh, char *param, const char *val)
{
   switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_text_param_tts invoke\n");
}

static void asinfo_tts_numeric_param_tts(switch_speech_handle_t *sh, char *param, int val)
{
   switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_numeric_param_tts invoke\n");
}

static void asinfo_tts_float_param_tts(switch_speech_handle_t *sh, char *param, double val)
{
   switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "asinfo_tts_float_param_tts invoke\n");
}

static switch_status_t load_config(void)
{
	char *cf = "asinfo_tts.conf";
	switch_xml_t cfg, xml = NULL, param, settings;

	memset((char*)&globals,0,sizeof(globals));


	if ((xml = switch_xml_open_cfg(cf, &cfg, NULL))) {
		if ((settings = switch_xml_child(cfg, "settings"))) {
			for (param = switch_xml_child(settings, "param"); param; param = param->next) {
				char *var = (char *) switch_xml_attr_soft(param, "name");
				char *val = (char *) switch_xml_attr_soft(param, "value");
				if (!strcasecmp(var, "url")) {
					strncpy(globals.url,val,128);
				} else if (!strcasecmp(var,"native_rate"))
				{
					globals.native_rate = atoi(val);
				}else  if (!strcasecmp(var,"speed"))
				{
                  globals.voice_speed = atof(val);
				}else if(!strcasecmp(var,"volume"))
				{
					globals.voice_volume = atof(val);
				}else
				{
					switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_WARNING, "Param \"%s\" unknown\n", var);
				}
			}
		}
        } else {
		switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_WARNING, "Open of \"%s\" failed. Using default settings.\n", cf);
	}
	if (!globals.native_rate)
	{
		globals.native_rate = 24000;
	}

	if (!globals.voice_speed)
	{
		globals.voice_speed = 1.0;
	}

	if (!globals.voice_volume)
	{
		globals.voice_volume = 1.0;
	}

	if (xml) {
		switch_xml_free(xml);
	}

	return SWITCH_STATUS_SUCCESS;
}

SWITCH_MODULE_LOAD_FUNCTION(mod_asinfo_tts_load)
{
	switch_speech_interface_t *speech_interface;
	
	/*---load config---*/
	memset(&globals, 0, sizeof(globals));
	load_config();
	switch_log_printf(SWITCH_CHANNEL_LOG, SWITCH_LOG_ERROR, "load http url :%s \n",globals.url);

	/* connect my internal structure to the blank pointer passed to me */
	*module_interface = switch_loadable_module_create_module_interface(pool, modname);
	speech_interface = switch_loadable_module_create_interface(*module_interface, SWITCH_SPEECH_INTERFACE);
	speech_interface->interface_name = "asinfo_tts";
	speech_interface->speech_open = asinfo_tts_speech_open;
	speech_interface->speech_close = asinfo_tts_speech_close;
	speech_interface->speech_feed_tts = asinfo_tts_speech_feed_tts;
	speech_interface->speech_read_tts = asinfo_tts_speech_read_tts;
	speech_interface->speech_flush_tts = asinfo_tts_speech_flush_tts;
	speech_interface->speech_text_param_tts = asinfo_tts_text_param_tts;
	speech_interface->speech_numeric_param_tts = asinfo_tts_numeric_param_tts;
	speech_interface->speech_float_param_tts = asinfo_tts_float_param_tts;

	/* indicate that the module should continue to be loaded */
	return SWITCH_STATUS_SUCCESS;
}

SWITCH_MODULE_SHUTDOWN_FUNCTION(mod_asinfo_tts_shutdown)
{
	return SWITCH_STATUS_UNLOAD;
}

/* For Emacs:
 * Local Variables:
 * mode:c
 * indent-tabs-mode:t
 * tab-width:4
 * c-basic-offset:4
 * End:
 * For VIM:
 * vim:set softtabstop=4 shiftwidth=4 tabstop=4 noet:
 */
